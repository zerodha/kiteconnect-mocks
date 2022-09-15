// To parse this JSON:
//
//   NSError *error;
//   QTMFSIPCancel *mFSIPCancel = [QTMFSIPCancel fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTMFSIPCancel;
@class QTDefinitions;
@class QTData;
@class QTDataProperties;
@class QTSipid;
@class QTMFSIPCancelClass;
@class QTMFSIPCancelProperties;
@class QTDataClass;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface QTMFSIPCancel : NSObject
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
@property (nonatomic, strong) QTMFSIPCancelClass *mfsipCancel;
@end

@interface QTData : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDataProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataProperties : NSObject
@property (nonatomic, strong) QTSipid *sipID;
@end

@interface QTSipid : NSObject
@property (nonatomic, copy) NSString *type;
@end

@interface QTMFSIPCancelClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTMFSIPCancelProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTMFSIPCancelProperties : NSObject
@property (nonatomic, strong) QTDataClass *data;
@property (nonatomic, strong) QTSipid *status;
@end

@interface QTDataClass : NSObject
@property (nonatomic, copy) NSString *ref;
@end

NS_ASSUME_NONNULL_END
