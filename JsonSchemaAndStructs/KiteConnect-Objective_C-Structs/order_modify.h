// To parse this JSON:
//
//   NSError *error;
//   QTOrderModify *orderModify = [QTOrderModify fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTOrderModify;
@class QTDefinitions;
@class QTData;
@class QTDataProperties;
@class QTOrderID;
@class QTOrderModifyClass;
@class QTOrderModifyProperties;
@class QTDataClass;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface QTOrderModify : NSObject
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
@property (nonatomic, strong) QTOrderModifyClass *orderModify;
@end

@interface QTData : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDataProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataProperties : NSObject
@property (nonatomic, strong) QTOrderID *orderID;
@end

@interface QTOrderID : NSObject
@property (nonatomic, copy) NSString *type;
@end

@interface QTOrderModifyClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTOrderModifyProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTOrderModifyProperties : NSObject
@property (nonatomic, strong) QTDataClass *data;
@property (nonatomic, strong) QTOrderID *status;
@end

@interface QTDataClass : NSObject
@property (nonatomic, copy) NSString *ref;
@end

NS_ASSUME_NONNULL_END
