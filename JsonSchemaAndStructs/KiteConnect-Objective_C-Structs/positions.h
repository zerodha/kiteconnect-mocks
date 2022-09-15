// To parse this JSON:
//
//   NSError *error;
//   QTPositions *positions = [QTPositions fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTPositions;
@class QTDefinitions;
@class QTData;
@class QTDataProperties;
@class QTDay;
@class QTDataClass;
@class QTDayClass;
@class QTProperty;
@class QTType;
@class QTPositionsClass;
@class QTPositionsProperties;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Boxed enums

@interface QTType : NSObject
@property (nonatomic, readonly, copy) NSString *value;
+ (instancetype _Nullable)withValue:(NSString *)value;
+ (QTType *)integer;
+ (QTType *)number;
+ (QTType *)string;
@end

#pragma mark - Object interfaces

@interface QTPositions : NSObject
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
@property (nonatomic, strong) QTDayClass *day;
@property (nonatomic, strong) QTPositionsClass *positions;
@end

@interface QTData : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDataProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataProperties : NSObject
@property (nonatomic, strong) QTDay *day;
@property (nonatomic, strong) QTDay *net;
@end

@interface QTDay : NSObject
@property (nonatomic, strong) QTDataClass *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataClass : NSObject
@property (nonatomic, copy) NSString *ref;
@end

@interface QTDayClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, copy)   NSDictionary<NSString *, QTProperty *> *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTProperty : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTPositionsClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTPositionsProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTPositionsProperties : NSObject
@property (nonatomic, strong) QTDataClass *data;
@property (nonatomic, strong) QTProperty *status;
@end

NS_ASSUME_NONNULL_END
