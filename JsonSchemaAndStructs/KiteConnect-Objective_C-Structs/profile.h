// To parse this JSON:
//
//   NSError *error;
//   QTProfile *profile = [QTProfile fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTProfile;
@class QTDefinitions;
@class QTData;
@class QTDataProperties;
@class QTAvatarURL;
@class QTType;
@class QTExchanges;
@class QTMeta;
@class QTMetaClass;
@class QTMetaProperties;
@class QTProfileClass;
@class QTProfileProperties;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Boxed enums

@interface QTType : NSObject
@property (nonatomic, readonly, copy) NSString *value;
+ (instancetype _Nullable)withValue:(NSString *)value;
+ (QTType *)null;
+ (QTType *)string;
@end

#pragma mark - Object interfaces

@interface QTProfile : NSObject
@property (nonatomic, copy)   NSString *ref;
@property (nonatomic, copy)   NSString *schema;
@property (nonatomic, strong) QTDefinitions *definitions;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface QTDefinitions : NSObject
@property (nonatomic, strong) QTData *data;
@property (nonatomic, strong) QTMetaClass *meta;
@property (nonatomic, strong) QTProfileClass *profile;
@end

@interface QTData : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDataProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataProperties : NSObject
@property (nonatomic, strong) QTAvatarURL *avatarURL;
@property (nonatomic, strong) QTAvatarURL *broker;
@property (nonatomic, strong) QTAvatarURL *email;
@property (nonatomic, strong) QTExchanges *exchanges;
@property (nonatomic, strong) QTMeta *meta;
@property (nonatomic, strong) QTExchanges *orderTypes;
@property (nonatomic, strong) QTExchanges *products;
@property (nonatomic, strong) QTAvatarURL *userID;
@property (nonatomic, strong) QTAvatarURL *userName;
@property (nonatomic, strong) QTAvatarURL *userShortname;
@property (nonatomic, strong) QTAvatarURL *userType;
@end

@interface QTAvatarURL : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTExchanges : NSObject
@property (nonatomic, strong) QTAvatarURL *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMeta : NSObject
@property (nonatomic, copy) NSString *ref;
@end

@interface QTMetaClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTMetaProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMetaProperties : NSObject
@property (nonatomic, strong) QTAvatarURL *dematConsent;
@end

@interface QTProfileClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTProfileProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTProfileProperties : NSObject
@property (nonatomic, strong) QTMeta *data;
@property (nonatomic, strong) QTAvatarURL *status;
@end

NS_ASSUME_NONNULL_END
